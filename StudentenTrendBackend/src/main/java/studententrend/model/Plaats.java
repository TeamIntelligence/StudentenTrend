package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SecondaryTable;
import javax.persistence.Table;

import org.hibernate.annotations.ForeignKey;

@Entity
@Table(name="PLAATSEN")
@SecondaryTable(name="PROVINCIES", pkJoinColumns = @PrimaryKeyJoinColumn(name="PROV_ID"))
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
	
	public int getPlaatsId() {
		return plaatsId;
	}

	public String getPlaatsNaam() {
		return plaatsNaam;
	}

	public Provincie getProvincie() {
		return provincie;
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
