package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name="SBI_CODES93")
public class SbiCode93Conv {
	
	@Id
	@Column(name="SBI_CODE93")
	private String sbiCode93;
	
	@Column(name="SBI_NAAM93")
	private String sbiNaam93;
	
	protected SbiCode93Conv(){}
	
	public SbiCode93Conv(String sbiCode93, String sbiNaam93){
		this.sbiCode93 = sbiCode93;
		this.sbiNaam93 = sbiNaam93;
	}
	
	@Override
	public String toString(){
		return String.format("SBI_CODE [SBI_CODE='%s', SBI_NAAM='%s']",
				sbiCode93, sbiNaam93);
	}
	
	public String getSbiCode93() {
		return sbiCode93;
	}

	public String getSbiNaam93() {
		return sbiNaam93;
	}
}
