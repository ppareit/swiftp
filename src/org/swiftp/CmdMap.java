package org.swiftp;

public class CmdMap {
	protected Class<? extends FtpCmd> cmdClass;
	String name;
	
	
	public CmdMap(String name, Class<? extends FtpCmd> cmdClass) {
		super();
		this.name = name;
		this.cmdClass = cmdClass;
	}

	public Class<? extends FtpCmd> getCommand() {
		return cmdClass;
	}

	public void setCommand(Class<? extends FtpCmd> cmdClass) {
		this.cmdClass = cmdClass;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
