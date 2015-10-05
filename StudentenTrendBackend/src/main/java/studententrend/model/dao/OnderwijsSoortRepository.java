package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.OnderwijsSoort;

public interface OnderwijsSoortRepository extends CrudRepository<OnderwijsSoort, Long>{
	List<OnderwijsSoort> findByOndCode(String ondCode);
	List<OnderwijsSoort> findByOndNaam(String ondNaam);
}
