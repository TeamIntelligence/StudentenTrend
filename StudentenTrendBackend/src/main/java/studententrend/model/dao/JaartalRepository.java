package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.Jaartal;


public interface JaartalRepository extends CrudRepository<Jaartal, Long>{
	List<Jaartal> findByJaartal(int jaartal);
}
