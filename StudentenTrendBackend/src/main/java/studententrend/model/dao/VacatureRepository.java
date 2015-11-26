package studententrend.model.dao;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import studententrend.model.SbiCode;
import studententrend.model.Vacature;

public interface VacatureRepository extends CrudRepository<Vacature, String> {
	
	
	List<Vacature> findByVacId(long vacId);
	List<Vacature> findByJaartal(int jaartal);
	List<Vacature> findByKwartaal(int kwartaal);
	List<Vacature> findBySbiCode(SbiCode sbiCode);
	List<Vacature> findByAantal(int aantal);
	
	@Query(value = 	"SELECT 	 V.VAC_ID 	VAC_ID " +
					"			,V.JAARTAL 	JAARTAL " +
					"			,V.SBI_CODE	SBI_CODE " +
					"			,V.KWARTAAL	KWARTAAL " +
					"			,SUM(AANTAL) AANTAL " + 
					"FROM 		VACATURES V " +
					"GROUP BY 	V.JAARTAL, V.SBI_CODE ", nativeQuery = true)
	List<Vacature> findByAlleJaartal();
}