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
@Table(name="SBI_CODES93")
public class SbiCode93 {
		
	@Id
	@Column(name="SBI_CODE93")
	private String sbiCode93;
	
	@Column(name="SBI_NAAM93")
	private String sbiNaam93;

	@ManyToMany(cascade = CascadeType.ALL)
	@JoinTable(name="SBI93_SBI_CODES", joinColumns={
		@JoinColumn(name="SBI_CODE93", nullable=false, updatable=false)
	}, inverseJoinColumns={@JoinColumn(name="SBI_CODE", nullable=false, updatable=false)})
	private Set<SbiCodeConv> sbiCodes = new HashSet<SbiCodeConv>(0);
	
	protected SbiCode93(){}
	
	public SbiCode93(String sbiCode93, String sbiNaam93){
		this.sbiCode93 = sbiCode93;
		this.sbiNaam93 = sbiNaam93;
	}
	
	@Override
	public String toString(){
		return String.format("SBI_CODE [SBI_CODE='%s', SBI_NAAM='%s']",
				sbiCode93, sbiNaam93);
	}
	
	public String getSbiCode93() {
		return sbiCode93;
	}

	public String getSbiNaam93() {
		return sbiNaam93;
	}
	
	public Set<SbiCodeConv> getSbiCodes() {
		return sbiCodes;
	}
	
	public void setIscedCodes(Set<SbiCodeConv> sbiCodes) {
		this.sbiCodes = sbiCodes;
	}
}
