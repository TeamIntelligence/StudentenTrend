package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name="PLAATSEN")
public class Plaats {
	
	@Id
	@Column(name="PLAATS_ID")
	private int plaatsId;

	@ManyToOne
	@JoinColumn(name="PROV_ID")
	private Provincie provincie;

	@Column(name="PLAATS_NAAM")
	private String plaatsNaam;
	
	protected Plaats(){}
	
	public Plaats(int plaatsId, Provincie provincie, String plaatsNaam) {
		this.plaatsId = plaatsId;
		this.provincie = provincie;
		this.plaatsNaam = plaatsNaam;
	}

	@Override
	public String toString(){
		return String.format("PLAATSEN [PLAATS_ID='%d', PROVINCIE=%s, PLAATS_NAAM=%s]", plaatsId, provincie.getProvNaam(), plaatsNaam);
	}
	
	public String getPlaatsNaam() {
		return plaatsNaam;
	}

	public String getProvincie() {
		return provincie.getProvNaam();
	}

//	public void setPlaatsId(int plaatsId) {
//		this.plaatsId = plaatsId;
//	}

//	public void setProvId(Provincie provincie) {
//		this.provincie = provincie;
//	}

//	public void setPlaatsNaam(String plaatsNaam) {
//		this.plaatsNaam = plaatsNaam;
//	}
}
