package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.Ingeschrevenen;
import studententrend.model.IscedCode;

public interface IngeschrevenenRepository extends CrudRepository<Ingeschrevenen, Long>{
	List<Ingeschrevenen> findByHiId(long hiId);
	List<Ingeschrevenen> findByJaartal(int jaartal);
	List<Ingeschrevenen> findByOndCode(String ondCode);
	List<Ingeschrevenen> findByIscedCode(IscedCode iscedCode);
	List<Ingeschrevenen> findByAantal(int aantal);
}
