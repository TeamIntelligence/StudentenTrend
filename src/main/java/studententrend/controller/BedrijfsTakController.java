package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
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
			System.out.println(bedrijfsTak.toString());
		}
		
		return bedrijfsTakken;
	}
}
