package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.Gediplomeerden;
import studententrend.model.IscedCode;

public interface GediplomeerdenRepository extends CrudRepository<Gediplomeerden, Long>{
	List<Gediplomeerden> findByHgId(long hgId);
	List<Gediplomeerden> findByJaartal(int jaartal);
	List<Gediplomeerden> findByOndCode(String ondCode);
	List<Gediplomeerden> findByIscedCode(IscedCode iscedCode);
	List<Gediplomeerden> findByDiploma(String diploma);
	List<Gediplomeerden> findByAantal(int aantal);
}
