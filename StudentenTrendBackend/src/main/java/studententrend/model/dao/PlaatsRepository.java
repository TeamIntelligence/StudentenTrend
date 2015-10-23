package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.Plaats;

public interface PlaatsRepository extends CrudRepository<Plaats, Long>{
	List<Plaats> findByPlaatsId(int plaatsId);
	List<Plaats> findByPlaatsNaam(String plaatsnaam);
	List<Plaats> findByProvincie(String provincie);
}
