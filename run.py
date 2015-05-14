#!/usr/bin/env python3

from subprocess import check_call, Popen, PIPE
import os
import json
import sys


def pp_miliseconds(miliseconds):
    hours, miliseconds = divmod(miliseconds, 3600000)
    minutes, miliseconds = divmod(miliseconds, 60000)
    seconds = float(miliseconds) / 1000
    s = "%i:%02i:%06.3f" % (hours, minutes, seconds)
    return s


def save_report(json_document, file_name):
    with open(file_name, 'w') as output_file:
        json_str = json.dumps(json_document, sort_keys=True, indent=4)
        output_file.write(json_str)


def generate_config_file(components, timeout, bound):
    '''
    Supported components levels:
      'constants', 'alternatives', 'booleans',
      'comparison', 'arithmetics', 'variables'
    '''
    dir = "var"
    if not os.path.exists(dir):
        os.makedirs(dir)
    file_name = dir + "/" + components + "-" + str(timeout) + "-" + str(bound) + ".json"
    if components == 'booleans':
        level = 'logical'
    else:
        level = 'linear'
    if components == 'constants':
        repairConstants = True
    else:
        repairConstants = False
    config = {"encodingConfig": {"componentsMultipleOccurrences": True,
                                 "phantomComponents": True,
                                 "repairBooleanConst": repairConstants,
                                 "repairIntegerConst": repairConstants,
                                 "level": level},
              "simplification": False,
              "spaceReduction": True,
              "componentLevel": components,
              "solverBound": bound,
              "solverTimeout": timeout}
    save_report(config, file_name)
    return file_name


def build_project():
    print("building project...")
    devnull = open(os.devnull, 'w')
    status = check_call(SBT_EXE + " assembly", shell=True, stdout=devnull)
    if status:
        print("build failed")


BENCHMARKS = {'tcas': [7, 13, 19, 24], # range(1, 42),
              'coreutils': [# '6856089f',
                            '72d05289',
                            '7cb24684',
                            'a0851554',
                            'b58a8b4e'],
              'replace': [1, 3, 4, 5, 7, 11, 16, 25, 26, 28, 29], #TODO: 27, 30
              'schedule': [1, 2, 4, 6, 9], #TODO 3, 7
              'schedule2': [5, 6, 7],
              'grep': []}


def run_benchmark(benchmark):
    timeout = 50000
    bound = 3
    components_levels = ['alternatives', 'arithmetics', 'booleans', 'variables'] # ['constants']
    config_files = [generate_config_file(components, timeout, bound)
                    for components in components_levels]
    successful_runs = 0
    results = []

    for version in BENCHMARKS[benchmark]:
        print("running {0} version {1}".format(benchmark, version))
        version_fixed = False
        command = "{0} -jar {1}".format(JAVA_EXE, MAXSAT_REPAIR_JAR)

        def command_args(config):
            return "-b {0} -v {1} -c {2} -q".format(benchmark, version, config)

        def run_repair(config):
            return Popen(command + " " + command_args(config),
                         stdout=PIPE,
                         shell=True)

        try:
            procs = [run_repair(config) for config in config_files]
            for proc in procs:
                proc.wait()
            for proc in procs:
                output = proc.stdout.read()
                result = json.loads(output.decode(encoding='UTF-8'))
                time = str(pp_miliseconds(int(result['rcSolvingTime'])))
                results.append(result)
                if result['fixed']:
                    version_fixed = True
                    print(str(len(result['patch'])) + "-line fix " + time)
                    for change in result['patch']:
                        print(change)
                else:
                    if result['isTimeout']:
                        print("timeout")
                    else:
                        print("failed")
        except Exception as e:
            info = "error while running {0} v{1}".format(benchmark, version)
            print(info + ": " + str(e))
        if version_fixed:
            successful_runs += 1

    totally = len(BENCHMARKS[benchmark])
    print("{0} out of {1} are repaired".format(successful_runs, totally))
    for file in config_files:
        os.remove(file)

    return results


if __name__ == "__main__":
    try:
        JDK_HOME = os.environ['JDK_HOME']
        JAVA_EXE = JDK_HOME + "/bin/java"
    except:
        JAVA_EXE = "java"
    print("using java executable: {0}".format(JAVA_EXE))
    SBT_EXE = "sbt"
    MAXSAT_REPAIR_JAR = "target/scala-2.10/repair-maxsat-assembly-1.0.jar"

    build_project()
    results = run_benchmark(sys.argv[1])
    save_report(results, "result.json")
