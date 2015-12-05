package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name="GEDIPLOMEERDEN_VACATURES")
public class GediplomeerdenVacatures {
		
	@Id
	@Column(name="GEDIPL_VAC_ID")
	private int gediplVacId;
	
	@Column(name="OND_CODE")
	private String ondCode;
	
	@Column(name="JAARTALLEN")
	private int jaartal;

	@ManyToOne
	@JoinColumn(name="SBI_CODE93")
	private SbiCode93 sbiCode93;

	@ManyToOne
	@JoinColumn(name="SOI_CODE")
	private SoiCode soiCode;
	
	@Column(name="DIRECT")
	private int direct;
	
	@Column(name="BINNEN_EEN_JAAR")
	private int binnenEenJaar;
	
	@Column(name="BINNEN_TWEE_JAAR")
	private int binnenTweeJaar;
	
	@Column(name="BINNEN_DRIE_JAAR")
	private int binnenDrieJaar;
	
	protected GediplomeerdenVacatures(){}
	
	
	
	public GediplomeerdenVacatures(int gediplVacId, String ondCode,
			int jaartal, SbiCode93 sbiCode93, SoiCode soiCode, int direct,
			int binnenEenJaar, int binnenTweeJaar, int binnenDrieJaar) {
		this.gediplVacId = gediplVacId;
		this.ondCode = ondCode;
		this.jaartal = jaartal;
		this.sbiCode93 = sbiCode93;
		this.soiCode = soiCode;
		this.direct = direct;
		this.binnenEenJaar = binnenEenJaar;
		this.binnenTweeJaar = binnenTweeJaar;
		this.binnenDrieJaar = binnenDrieJaar;
	}
	
	@Override
	public String toString() {
		return "GediplomeerdenVacatures [gediplVacId=" + gediplVacId
				+ ", ondCode=" + ondCode + ", jaartal=" + jaartal
				+ ", sbiCode93=" + sbiCode93 + ", soiCode=" + soiCode
				+ ", direct=" + direct + ", binnenEenJaar=" + binnenEenJaar
				+ ", binnenTweeJaar=" + binnenTweeJaar + ", binnenDrieJaar="
				+ binnenDrieJaar + "]";
	}

	public int getGediplVacId() {
		return gediplVacId;
	}

	public String getOndCode() {
		return ondCode;
	}

	public int getJaartal() {
		return jaartal;
	}

	public SbiCode93 getSbiCode93() {
		return sbiCode93;
	}

	public SoiCode getSoiCode() {
		return soiCode;
	}

	public int getDirect() {
		return direct;
	}

	public int getBinnenEenJaar() {
		return binnenEenJaar;
	}

	public int getBinnenTweeJaar() {
		return binnenTweeJaar;
	}

	public int getBinnenDrieJaar() {
		return binnenDrieJaar;
	}
}