package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name="ISCED_CODES")
public class IscedCodeConv {
	
	@Id
	@Column(name = "ISCED_CODE")
	private long iscedCode;

	@Column(name = "ISCED_NAAM")
	private String iscedNaam;
	
	protected IscedCodeConv(){}

	public IscedCodeConv(long iscedCode, String iscedNaam){
		this.iscedCode = iscedCode;
		this.iscedNaam = iscedNaam;
	}
	
	@Override
	public String toString(){
		return String.format("SBI_CODE [ISCED_CODE='%s', ISCED_NAAM='%s']",
				iscedCode, iscedNaam);
	}

	public long getIscedCode() {
		return iscedCode;
	}

	public String getIscedNaam() {
		return iscedNaam;
	}
}
