package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.Jaartal;
import studententrend.model.Kwartaal;
import studententrend.model.SbiCode;
import studententrend.model.Vacature;

public interface VacatureRepository extends CrudRepository<Vacature, String> {
	List<Vacature> findByVacId(long vacId);
	List<Vacature> findByJaartal(Jaartal jaartal);
	List<Vacature> findByKwartaal(Kwartaal kwartaal);
	List<Vacature> findBySbiCode(SbiCode sbiCode);
	List<Vacature> findByAantal(int aantal);
}