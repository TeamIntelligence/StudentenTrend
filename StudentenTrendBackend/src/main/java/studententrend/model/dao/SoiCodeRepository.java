package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.SoiCode;

public interface SoiCodeRepository extends CrudRepository<SoiCode, String> {
	List<SoiCode> findBySoiCode(int soiCode);
	List<SoiCode> findBySoiNaam(String soiNaam);
}
