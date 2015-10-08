package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.StudieVoortgang;

public interface StudieVoortgangRepository extends CrudRepository<StudieVoortgang, Long>{
	List<StudieVoortgang> findBySvId(long svId);
	List<StudieVoortgang> findByIscedCode(long IscedCode);
}
