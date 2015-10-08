package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name="HO_INGESCHREVENEN")
public class Ingeschrevenen {
	@Id
	@Column(name = "HI_ID")
	private long hiId;

	@ManyToOne
	@JoinColumn(name="JAARTAL")
	private Jaartal jaartal;

	@ManyToOne
	@JoinColumn(name="OND_CODE")
	private OnderwijsSoort ondCode;

	@ManyToOne
	@JoinColumn(name="ISCED_CODE")
	private IscedCode iscedCode;

	@Column(name = "AANTAL")
	private int aantal;
	
	protected Ingeschrevenen(){}
	
	public Ingeschrevenen(long hiId, Jaartal jaartal, OnderwijsSoort ondCode, IscedCode iscedCode, String diploma, int aantal) {
		this.hiId = hiId;
		this.jaartal = jaartal;
		this.ondCode = ondCode;
		this.iscedCode = iscedCode;
		this.aantal = aantal;
	}

	@Override
	public String toString(){
		return String.format("HO_INGESCHREVENEN [HI_ID='%d', JAARTAL='%d', OND_CODE=%s, ISCED_CODE=%d, AANTAL=%d]",
				hiId, jaartal.getJaartal(), ondCode.getOndCode(), iscedCode.getIscedCode(), aantal);
	}


	public long getHiId() {
		return hiId;
	}


	public int getJaartal() {
		return jaartal.getJaartal();
	}


	public String getOndCode() {
		return ondCode.getOndCode();
	}


	public IscedCode getIscedCode() {
		return iscedCode;
	}


	public int getAantal() {
		return aantal;
	}
}