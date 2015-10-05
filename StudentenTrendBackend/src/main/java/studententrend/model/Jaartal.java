package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name="JAARTALLEN")
public class Jaartal {
	
	@Id
	@Column(name="JAARTAL")
	private int jaartal;
	
	protected Jaartal(){}
	
	public Jaartal(int jaartal){
		this.jaartal = jaartal;
	}
	
	@Override
	public String toString(){
		return String.format("JAARTALLEN [JAARTAL='%d']", jaartal);
	}
	
	//Getters and Setters
	public int getJaartal() {
		return jaartal;
	}

//	public void setJaartal(int jaartal) {
//		this.jaartal = jaartal;
//	}
}
