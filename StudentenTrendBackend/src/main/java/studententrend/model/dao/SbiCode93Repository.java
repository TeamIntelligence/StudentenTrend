package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.SbiCode93;

public interface SbiCode93Repository extends CrudRepository<SbiCode93, String> {
	
	List<SbiCode93> findBySbiCode93(String sbiCode93);
	List<SbiCode93> findBySbiNaam93(String sbiNaam93);
}
