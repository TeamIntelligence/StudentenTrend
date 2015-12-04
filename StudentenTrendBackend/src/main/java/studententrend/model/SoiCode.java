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
@Table(name="SOI_CODES")
public class SoiCode {
	
	@Id
	@Column(name="SOI_CODE")
	private int soiCode;
	
	@Column(name="SOI_NAAM")
	private String soiNaam;

	@ManyToMany(cascade = CascadeType.ALL)
	@JoinTable(name="ISCED_SOI_CODES", joinColumns={
		@JoinColumn(name="SOI_CODE", nullable=false, updatable=false)
	}, inverseJoinColumns={@JoinColumn(name="ISCED_CODE", nullable=false, updatable=false)})
	private Set<IscedCodeConv> iscedCodes = new HashSet<IscedCodeConv>(0);
	
	protected SoiCode(){}
	
	public SoiCode(int soiCode, String soiNaam){
		this.soiCode = soiCode;
		this.soiNaam = soiNaam;
	}
	
	@Override
	public String toString(){
		return String.format("SOI_CODES [SOI_CODE='%s', SOI_NAAM='%s']",
				soiCode, soiNaam);
	}
	
	public int getSoiCode() {
		return soiCode;
	}

	public String getSoiNaam() {
		return soiNaam;
	}
	
	public Set<IscedCodeConv> getIscedCodes() {
		return iscedCodes;
	}
	
	public void setIscedCodes(Set<IscedCodeConv> iscedCodes) {
		this.iscedCodes = iscedCodes;
	}
}
