package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name="bedrijfs_takken")
public class BedrijfsTak {
	
	@Id
	@GeneratedValue(strategy=GenerationType.AUTO)
	@Column(name="bedrijfs_id")
	private long bedrijfsId;
	
	@Column(name="sector_code")
	private String sectorCode;
	
	@Column(name="sector_naam")
	private String sectorNaam;

	protected BedrijfsTak(){}
	
	public BedrijfsTak(String sectorCode, String sectorNaam){
		this.sectorCode = sectorCode;
		this.sectorNaam = sectorNaam;
	}
	
	@Override
	public String toString(){
		return String.format("Bedrijfstak [bedrijfs_id='%d', sector_code='%s', sector_naam='%s']",
				bedrijfsId, sectorCode, sectorNaam);
	}
	
	//Getters and Setters
	public long getBedrijfsId() {
		return bedrijfsId;
	}

	public void setBedrijfsId(long bedrijfsId) {
		this.bedrijfsId = bedrijfsId;
	}

	public String getSectorCode() {
		return sectorCode;
	}

	public void setSectorCode(String sectorCode) {
		this.sectorCode = sectorCode;
	}

	public String getSectorNaam() {
		return sectorNaam;
	}

	public void setSectorNaam(String sectorNaam) {
		this.sectorNaam = sectorNaam;
	}
}
