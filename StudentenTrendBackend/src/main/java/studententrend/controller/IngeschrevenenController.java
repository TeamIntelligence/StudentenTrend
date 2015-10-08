package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.Ingeschrevenen;
import studententrend.model.OnderwijsSoort;
import studententrend.model.dao.IngeschrevenenRepository;
import studententrend.model.dao.OnderwijsSoortRepository;

@RestController
@RequestMapping("/studenten")
public class IngeschrevenenController {
	@Autowired
	IngeschrevenenRepository ingeschrevenenRepository;

	@Autowired
	OnderwijsSoortRepository onderwijsSoortRepository;
	
	@RequestMapping(
			value = "/ingeschrevenen", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Ingeschrevenen> hoGediplomeerdenAll() {
		List<Ingeschrevenen> hoIngeschrevenen = (List<Ingeschrevenen>) ingeschrevenenRepository.findAll();
		
		for(Ingeschrevenen hoIngeschreven : hoIngeschrevenen){
			System.out.println(hoIngeschreven.toString() + '\n');
		}
		
		return hoIngeschrevenen;
	}
	
	@RequestMapping(
			value = "/ingeschrevenen/{ondCode}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Ingeschrevenen> hoGediplomeerdenOndCode(@PathVariable("ondCode") String ondCode) {
		List<OnderwijsSoort> onderwijs = (List<OnderwijsSoort>) onderwijsSoortRepository.findByOndCode(ondCode);
		List<Ingeschrevenen> ingeschrevenen = null;
		for(OnderwijsSoort soort : onderwijs){
			ingeschrevenen = (List<Ingeschrevenen>) ingeschrevenenRepository.findByOndCode(soort);	
		}
				
		return ingeschrevenen;
	}
	
}