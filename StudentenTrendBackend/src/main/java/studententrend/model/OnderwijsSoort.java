package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name="ONDERWIJS_SOORTEN")
public class OnderwijsSoort {
	
	@Id
	@Column(name="OND_CODE")
	private String ondCode;
	
	@Column(name="OND_NAAM")
	private String ondNaam;
	
	protected OnderwijsSoort(){}
	
	public OnderwijsSoort(String ondCode, String ondNaam){
		this.ondCode = ondCode;
		this.ondNaam = ondNaam;
	}
	
	@Override
	public String toString(){
		return String.format("ONDERWIJS_SOORTEN [OND_CODE='%s', OND_NAAM=%s]", ondCode, ondNaam);
	}
	
	//Getters and Setters
	public String getOndCode() {
		return ondCode;
	}

	//Getters and Setters
	public String getOndNaam() {
		return ondNaam;
	}

//	public void setOndCode(String ondCode) {
//		this.ondCode = ondCode;
//	}

//	public void setOndNaam(String ondNaam) {
//		this.ondNaam = ondNaam;
//	}
}
