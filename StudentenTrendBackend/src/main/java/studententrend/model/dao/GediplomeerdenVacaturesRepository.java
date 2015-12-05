package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.GediplomeerdenVacatures;
import studententrend.model.SbiCode93;
import studententrend.model.SoiCode;

public interface GediplomeerdenVacaturesRepository extends CrudRepository<GediplomeerdenVacatures, Long>{
	List<GediplomeerdenVacatures> findByGediplVacId(int gediplVacId);
	List<GediplomeerdenVacatures> findByOndCode(String ondCode);
	List<GediplomeerdenVacatures> findByJaartal(int jaartal);
	List<GediplomeerdenVacatures> findBySbiCode93(SbiCode93 sbiCode93);
	List<GediplomeerdenVacatures> findBySoiCode(SoiCode soiCode);
}