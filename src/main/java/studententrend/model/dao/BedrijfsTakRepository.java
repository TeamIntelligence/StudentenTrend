package studententrend.model.dao;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import studententrend.model.BedrijfsTak;

public interface BedrijfsTakRepository extends CrudRepository<BedrijfsTak, Long> {
	List<BedrijfsTak> findBySectorNaam(String sectorNaam);
}
