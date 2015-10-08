package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name="HO_GEDIPLOMEERDEN")
public class HoGediplomeerde {
	@Id
	@Column(name = "HG_ID")
	private long hgId;

	@ManyToOne
	@JoinColumn(name="JAARTAL")
	private Jaartal jaartal;

	@ManyToOne
	@JoinColumn(name="OND_CODE")
	private OnderwijsSoort ondCode;

	@ManyToOne
	@JoinColumn(name="ISCED_CODE")
	private IscedCode iscedCode;

	@Column(name = "DIPLOMA")
	private String diploma;

	@Column(name = "AANTAL")
	private int aantal;
	
	protected HoGediplomeerde(){}

	
	public HoGediplomeerde(long hgId, Jaartal jaartal, OnderwijsSoort ondCode, IscedCode iscedCode, String diploma, int aantal) {
		this.hgId = hgId;
		this.jaartal = jaartal;
		this.ondCode = ondCode;
		this.iscedCode = iscedCode;
		this.diploma = diploma;
		this.aantal = aantal;
	}

	@Override
	public String toString(){
		return String.format("HO_GEDIPLOMEERDEN [HG_ID='%d', JAARTAL='%d', OND_CODE=%s, ISCED_CODE=%d, DIPLOMA=%s, AANTAL=%d]",
				hgId, jaartal.getJaartal(), ondCode.getOndCode(), iscedCode.getIscedCode(), diploma, aantal);
	}


	public long getHgId() {
		return hgId;
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


	public String getDiploma() {
		return diploma;
	}


	public int getAantal() {
		return aantal;
	}
}
