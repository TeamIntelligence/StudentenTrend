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
@Table(name="SBI_CODES")
public class SbiCode {
	
	@Id
	@Column(name="SBI_CODE")
	private String sbiCode;
	
	@Column(name="SBI_NAAM")
	private String sbiNaam;

	@ManyToMany(cascade = CascadeType.ALL)
	@JoinTable(name="ISCED_SBI_CODES", joinColumns={
		@JoinColumn(name="SBI_CODE", nullable=false, updatable=false)
	}, inverseJoinColumns={@JoinColumn(name="ISCED_CODE", nullable=false, updatable=false)})
	private Set<IscedCodeConv> iscedCodes = new HashSet<IscedCodeConv>(0);
	
	protected SbiCode(){}
	
	public SbiCode(String sbiCode, String sbiNaam){
		this.sbiCode = sbiCode;
		this.sbiNaam = sbiNaam;
	}
	
	@Override
	public String toString(){
		return String.format("SBI_CODE [SBI_CODE='%s', SBI_NAAM='%s']",
				sbiCode, sbiNaam);
	}
	
	public String getSbiCode() {
		return sbiCode;
	}

	public String getSbiNaam() {
		return sbiNaam;
	}
	
	public Set<IscedCodeConv> getIscedCodes() {
		return iscedCodes;
	}
	
	public void setIscedCodes(Set<IscedCodeConv> iscedCodes) {
		this.iscedCodes = iscedCodes;
	}
}
