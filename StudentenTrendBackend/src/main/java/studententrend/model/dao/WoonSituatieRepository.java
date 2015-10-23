package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.Plaats;
import studententrend.model.WoonSituatie;

public interface WoonSituatieRepository extends CrudRepository<WoonSituatie, String> {
	List<WoonSituatie> findByWsId(long wsId);
	List<WoonSituatie> findByJaartal(int jaartal);
	List<WoonSituatie> findByKwartaal(int kwartaal);
	List<WoonSituatie> findByPlaats(Plaats plaats);
	List<WoonSituatie> findByOndCode(String ondCode);
	List<WoonSituatie> findByInwonend(int inwonend);
	List<WoonSituatie> findByUitwonend(int uitwonend);
}