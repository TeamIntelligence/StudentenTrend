package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name="VACATURES")
public class Vacature {
	@Id
	@Column(name = "VAC_ID")
	private long vacId;

	@ManyToOne
	@JoinColumn(name="JAARTAL")
	private Jaartal jaartal;

	@ManyToOne
	@JoinColumn(name="KWARTAAL")
	private Kwartaal kwartaal;
	
	@ManyToOne
	@JoinColumn(name="SBI_CODE")
	private SbiCode sbiCode;

	@Column(name = "AANTAL")
	private int aantal;
	
	protected Vacature(){}


	public Vacature(long vacId, Jaartal jaartal, Kwartaal kwartaal, SbiCode sbiCode, int aantal) {
		this.vacId = vacId;
		this.jaartal = jaartal;
		this.kwartaal = kwartaal;
		this.sbiCode = sbiCode;
		this.aantal = aantal;
	}


	@Override
	public String toString(){
		return String.format("VACATURES [VAC_ID='%d', JAARTAL='%d', KWARTAAL=%d, SBI_CODE=%s, AANTAL=%d]",
				vacId, jaartal.getJaartal(), kwartaal.getKwartaal(), sbiCode.getSbiNaam(), aantal);
	}


	public long getVacId() {
		return vacId;
	}


	public int getJaartal() {
		return jaartal.getJaartal();
	}


	public int getKwartaal() {
		return kwartaal.getKwartaal();
	}


	public SbiCode getSbiCode() {
		return sbiCode;
	}


	public int getAantal() {
		return aantal;
	}
	
	
}