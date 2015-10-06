package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.HoIngeschrevene;
import studententrend.model.IscedCode;
import studententrend.model.Jaartal;
import studententrend.model.OnderwijsSoort;

public interface HoIngeschreveneRepository extends CrudRepository<HoIngeschrevene, Long>{
	List<HoIngeschrevene> findByHiId(long hiId);
	List<HoIngeschrevene> findByJaartal(Jaartal jaartal);
	List<HoIngeschrevene> findByOndCode(OnderwijsSoort ondCode);
	List<HoIngeschrevene> findByIscedCode(IscedCode iscedCode);
	List<HoIngeschrevene> findByAantal(int aantal);
}
