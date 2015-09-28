package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.BedrijfsTak;
import studententrend.model.dao.BedrijfsTakRepository;

@RestController
public class BedrijfsTakController {

	@Autowired
	BedrijfsTakRepository bedrijfsTakRepository;

	@RequestMapping(
			value = "/bedrijfstak", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<BedrijfsTak> bedrijfsTak() {
		List<BedrijfsTak> bedrijfsTakken = (List<BedrijfsTak>) bedrijfsTakRepository.findAll();
		
		for(BedrijfsTak bedrijfsTak : bedrijfsTakken){
			System.out.println(bedrijfsTak.toString() + '\n');
		}
		
		return bedrijfsTakken;
	}
	
	@RequestMapping(
			value = "/bedrijfstak/{sector}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<BedrijfsTak> bedrijfsTak(@PathVariable String sector) {
		List<BedrijfsTak> bedrijfsTakken = null;
		if(sector.length() == 1) {
			bedrijfsTakken = (List<BedrijfsTak>) bedrijfsTakRepository.findBySectorCode(sector);
		} else {
			sector = sector.replaceAll("_", " ");
			bedrijfsTakken = (List<BedrijfsTak>) bedrijfsTakRepository.findBySectorNaam(sector);
		}		
		
		for(BedrijfsTak bedrijfsTak : bedrijfsTakken){
			System.out.println(bedrijfsTak.toString() + '\n');
		}
		
		return bedrijfsTakken;
	}
}
