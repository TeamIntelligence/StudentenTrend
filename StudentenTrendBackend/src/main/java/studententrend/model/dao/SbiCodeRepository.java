package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.SbiCode;

public interface SbiCodeRepository extends CrudRepository<SbiCode, String> {
	List<SbiCode> findBySbiCode(String sbiCode);
	List<SbiCode> findBySbiNaam(String sbiNaam);
}
