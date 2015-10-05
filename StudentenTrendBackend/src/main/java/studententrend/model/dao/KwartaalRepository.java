package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.Kwartaal;

public interface KwartaalRepository extends CrudRepository<Kwartaal, Long>{
	List<Kwartaal> findByKwartaal(int kwartaal);
}
