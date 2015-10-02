package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.IscedCode;
import studententrend.model.SbiCode;

public interface IscedCodeRepository extends CrudRepository<IscedCode, Long>{
	List<SbiCode> findByIscedCode(String iscedCode);
	List<SbiCode> findByIscedNaam(String iscedNaam);
}
