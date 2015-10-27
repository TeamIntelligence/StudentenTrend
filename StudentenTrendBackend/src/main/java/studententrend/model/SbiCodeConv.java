package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name="SBI_CODES")
public class SbiCodeConv {
	
	@Id
	@Column(name="SBI_CODE")
	private String sbiCode;
	
	@Column(name="SBI_NAAM")
	private String sbiNaam;
	
	protected SbiCodeConv(){}
	
	public SbiCodeConv(String sbiCode, String sbiNaam){
		this.sbiCode = sbiCode;
		this.sbiNaam = sbiNaam;
	}
	
	@Override
	public String toString(){
		return String.format("SBI_CODE [SBI_CODE='%s', SBI_NAAM='%s']",
				sbiCode, sbiNaam);
	}
	
	public String getSbiCode() {
		return sbiCode;
	}

	public String getSbiNaam() {
		return sbiNaam;
	}
}
