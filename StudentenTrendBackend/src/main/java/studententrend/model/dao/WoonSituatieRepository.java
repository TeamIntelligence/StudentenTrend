package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.Jaartal;
import studententrend.model.Kwartaal;
import studententrend.model.OnderwijsSoort;
import studententrend.model.Plaats;
import studententrend.model.WoonSituatie;

public interface WoonSituatieRepository extends CrudRepository<WoonSituatie, String> {
	List<WoonSituatie> findByWsId(long wsId);
	List<WoonSituatie> findByJaartal(Jaartal jaartal);
	List<WoonSituatie> findByKwartaal(Kwartaal kwartaal);
	List<WoonSituatie> findByPlaats(Plaats plaats);
	List<WoonSituatie> findByOndCode(OnderwijsSoort ondCode);
	List<WoonSituatie> findByInwonend(int inwonend);
	List<WoonSituatie> findByUitwonend(int uitwonend);
}