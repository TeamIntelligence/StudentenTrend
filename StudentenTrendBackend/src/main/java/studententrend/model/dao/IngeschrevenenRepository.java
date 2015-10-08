package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.Ingeschrevenen;
import studententrend.model.IscedCode;
import studententrend.model.Jaartal;
import studententrend.model.OnderwijsSoort;

public interface IngeschrevenenRepository extends CrudRepository<Ingeschrevenen, Long>{
	List<Ingeschrevenen> findByHiId(long hiId);
	List<Ingeschrevenen> findByJaartal(Jaartal jaartal);
	List<Ingeschrevenen> findByOndCode(OnderwijsSoort ondCode);
	List<Ingeschrevenen> findByIscedCode(IscedCode iscedCode);
	List<Ingeschrevenen> findByAantal(int aantal);
}
