package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.IscedCode;

public interface IscedCodeRepository extends CrudRepository<IscedCode, Long>{
	List<IscedCode> findByIscedCode(long iscedCode);
	List<IscedCode> findByIscedNaam(String iscedNaam);
}
