package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.Plaats;
import studententrend.model.Provincie;

public interface PlaatsRepository extends CrudRepository<Plaats, Long>{
	List<Plaats> findByPlaatsId(int plaatsId);
	List<Plaats> findByPlaatsNaam(String plaatsnaam);
	List<Plaats> findByProvincie(Provincie provincie);
}
