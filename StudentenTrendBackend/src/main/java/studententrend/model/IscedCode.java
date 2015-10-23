package studententrend.model;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.Table;


@Entity
@Table(name="ISCED_CODES")
public class IscedCode {
	
	@Id
	@Column(name = "ISCED_CODE")
	private long iscedCode;

	@Column(name = "ISCED_NAAM")
	private String iscedNaam;

	@ManyToMany(cascade = CascadeType.ALL)
	@JoinTable(name="ISCED_SBI_CODES", joinColumns={
		@JoinColumn(name="ISCED_CODE", nullable=false, updatable=false)
	}, inverseJoinColumns={@JoinColumn(name="SBI_CODE", nullable=false, updatable=false)})
	private Set<SbiCodeConv> sbiCodes = new HashSet<SbiCodeConv>(0);
	
	protected IscedCode(){}

	public IscedCode(long iscedCode, String iscedNaam){
		this.iscedCode = iscedCode;
		this.iscedNaam = iscedNaam;
	}
	
	@Override
	public String toString(){
		return String.format("SBI_CODE [ISCED_CODE='%s', ISCED_NAAM='%s']",
				iscedCode, iscedNaam);
	}

	public long getIscedCode() {
		return iscedCode;
	}

	public String getIscedNaam() {
		return iscedNaam;
	}

	public Set<SbiCodeConv> getSbiCodes() {
		return sbiCodes;
	}
	
	public void setSbiCodes(Set<SbiCodeConv> sbiCodes) {
		this.sbiCodes = sbiCodes;
	}
}
