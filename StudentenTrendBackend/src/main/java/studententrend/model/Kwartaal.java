package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name="KWARTALEN")
public class Kwartaal {
	
	@Id
	@Column(name="KWARTAAL")
	private int kwartaal;
	
	protected Kwartaal(){}
	
	public Kwartaal(int kwartaal){
		this.kwartaal = kwartaal;
	}
	
	@Override
	public String toString(){
		return String.format("KWARTALEN [KWARTAAL='%d']", kwartaal);
	}
	
	//Getters and Setters
	public int getKwartaal() {
		return kwartaal;
	}

//	public void setKwartaal(int kwartaal) {
//		this.kwartaal = kwartaal;
//	}
}
