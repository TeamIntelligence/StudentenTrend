package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.Provincie;

public interface ProvincieRepository extends CrudRepository<Provincie, Long>{
	List<Provincie> findByProvId(int provId);
	List<Provincie> findByProvNaam(String provNaam);
}
