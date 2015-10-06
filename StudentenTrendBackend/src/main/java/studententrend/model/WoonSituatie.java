package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name="WOON_SITUATIES")
public class WoonSituatie {
	@Id
	@Column(name = "WS_ID")
	private long wsId;

	@ManyToOne
	@JoinColumn(name="JAARTAL")
	private Jaartal jaartal;

	@ManyToOne
	@JoinColumn(name="KWARTAAL")
	private Kwartaal kwartaal;
	
	@ManyToOne
	@JoinColumn(name="PLAATS_ID")
	private Plaats plaats;

	@ManyToOne
	@JoinColumn(name="OND_CODE")
	private OnderwijsSoort ondCode;
	
	@Column(name = "INWONEND")
	private int inwonend;

	@Column(name = "UITWONEND")
	private int uitwonend;
	
	protected WoonSituatie(){}
	
	public WoonSituatie(long wsId, Jaartal jaartal, Kwartaal kwartaal, Plaats plaats, OnderwijsSoort ondCode, int inwonend, int uitwonend) {
		this.wsId = wsId;
		this.jaartal = jaartal;
		this.kwartaal = kwartaal;
		this.plaats = plaats;
		this.ondCode = ondCode;
		this.inwonend = inwonend;
		this.uitwonend = uitwonend;
	}

	@Override
	public String toString(){
		return String.format("WOON_SITUATIES [WS_ID='%d', JAARTAL='%d', KWARTAAL=%d, PLAATS=%s, OND_CODE=%s, INWONEND=%d, UITWONEND=%d]",
				wsId, jaartal.getJaartal(), kwartaal.getKwartaal(), plaats.getPlaatsNaam(), ondCode.getOndNaam(), inwonend, uitwonend);
	}

	public long getWsId() {
		return wsId;
	}

	public Jaartal getJaartal() {
		return jaartal;
	}

	public Kwartaal getKwartaal() {
		return kwartaal;
	}

	public Plaats getPlaats() {
		return plaats;
	}

	public OnderwijsSoort getOndCode() {
		return ondCode;
	}

	public int getInwonend() {
		return inwonend;
	}

	public int getUitwonend() {
		return uitwonend;
	}
}