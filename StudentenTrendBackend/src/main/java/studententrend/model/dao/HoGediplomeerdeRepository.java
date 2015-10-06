package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.HoGediplomeerde;
import studententrend.model.IscedCode;
import studententrend.model.Jaartal;
import studententrend.model.OnderwijsSoort;

public interface HoGediplomeerdeRepository extends CrudRepository<HoGediplomeerde, Long>{
	List<HoGediplomeerde> findByHgId(long hgId);
	List<HoGediplomeerde> findByJaartal(Jaartal jaartal);
	List<HoGediplomeerde> findByOndCode(OnderwijsSoort ondCode);
	List<HoGediplomeerde> findByIscedCode(IscedCode iscedCode);
	List<HoGediplomeerde> findByDiploma(String diploma);
	List<HoGediplomeerde> findByAantal(int aantal);
}
