package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.Gediplomeerden;
import studententrend.model.IscedCode;
import studententrend.model.Jaartal;
import studententrend.model.OnderwijsSoort;

public interface GediplomeerdenRepository extends CrudRepository<Gediplomeerden, Long>{
	List<Gediplomeerden> findByHgId(long hgId);
	List<Gediplomeerden> findByJaartal(Jaartal jaartal);
	List<Gediplomeerden> findByOndCode(OnderwijsSoort ondCode);
	List<Gediplomeerden> findByIscedCode(IscedCode iscedCode);
	List<Gediplomeerden> findByDiploma(String diploma);
	List<Gediplomeerden> findByAantal(int aantal);
}
