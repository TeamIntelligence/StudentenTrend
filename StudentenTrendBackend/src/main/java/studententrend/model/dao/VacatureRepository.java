package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.SbiCode;
import studententrend.model.Vacature;

public interface VacatureRepository extends CrudRepository<Vacature, String> {
	List<Vacature> findByVacId(long vacId);
	List<Vacature> findByJaartal(int jaartal);
	List<Vacature> findByKwartaal(int kwartaal);
	List<Vacature> findBySbiCode(SbiCode sbiCode);
	List<Vacature> findByAantal(int aantal);
}