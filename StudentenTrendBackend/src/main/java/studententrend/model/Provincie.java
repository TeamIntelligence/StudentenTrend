package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name="PROVINCIES")
public class Provincie {

	@Id
	@Column(name="PROV_ID")
	private int provId;

	@Column(name="PROV_NAAM")
	private String provNaam;

	protected Provincie(){}

	public Provincie(int provId, String provNaam) {
		this.provId = provId;
		this.provNaam = provNaam;
	}

	@Override
	public String toString(){
		return String.format("PROVINCIES [PROV_ID=%d, PROV_NAAM=%s]", provId, provNaam);
	}

	public int getProvId() {
		return provId;
	}

	public String getProvNaam() {
		return provNaam;
	}

//	public void setProvId(int provId) {
//		this.provId = provId;
//	}

//	public void setProvNaam(String provNaam) {
//		this.provNaam = provNaam;
//	}
}
