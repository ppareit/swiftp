package org.swiftp;

public class QuotaStats {
	private int quota;
	private int used;
	
	public QuotaStats(int used, int quota) {
		this.quota = quota;
		this.used = used;
	}
	
	public int getQuota() {
		return quota;
	}
	public int getUsed() {
		return used;
	}
}
