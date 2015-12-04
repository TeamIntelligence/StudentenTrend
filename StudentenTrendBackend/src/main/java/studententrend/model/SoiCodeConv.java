package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name="SOI_CODES")
public class SoiCodeConv {
	
	@Id
	@Column(name="SOI_CODE")
	private int soiCode;
	
	@Column(name="SOI_NAAM")
	private String soiNaam;
	
	protected SoiCodeConv(){}
	
	public SoiCodeConv(int soiCode, String soiNaam){
		this.soiCode = soiCode;
		this.soiNaam = soiNaam;
	}
	
	@Override
	public String toString(){
		return String.format("SOI_CODES [SOI_CODE='%s', SOI_NAAM='%s']",
				soiCode, soiNaam);
	}
	
	public int getSoiCode() {
		return soiCode;
	}

	public String getSoiNaam() {
		return soiNaam;
	}
}
