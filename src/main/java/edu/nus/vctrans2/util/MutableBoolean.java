package edu.nus.vctrans2.util;

public class MutableBoolean {

	private boolean value;
	
	public MutableBoolean(boolean value) {
		this.value = value;
	}
	
	public boolean value() {
		return this.value;
	}
	
	public void setValue(boolean value) {
		this.value = value;
	}
	
	@Override
	public String toString() {
		return new Boolean(this.value).toString();
	}
}
