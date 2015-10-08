package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.Gediplomeerden;
import studententrend.model.OnderwijsSoort;
import studententrend.model.dao.GediplomeerdenRepository;
import studententrend.model.dao.OnderwijsSoortRepository;

@RestController
@RequestMapping("/studenten")
public class GediplomeerdenController {
	@Autowired
	GediplomeerdenRepository gediplomeerdenRepository;

	@Autowired
	OnderwijsSoortRepository onderwijsSoortRepository;
	
	@RequestMapping(
			value = "/gediplomeerden", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Gediplomeerden> gediplomeerdenAll() {
		List<Gediplomeerden> hoGediplomeerden = (List<Gediplomeerden>) gediplomeerdenRepository.findAll();
		
		for(Gediplomeerden hoGediplomeerde : hoGediplomeerden){
			System.out.println(hoGediplomeerde.toString() + '\n');
		}
		
		return hoGediplomeerden;
	}

	@RequestMapping(
			value = "/gediplomeerden/{ondCode}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Gediplomeerden> hoGediplomeerdenOndCode(@PathVariable("ondCode") String ondCode) {
		List<OnderwijsSoort> onderwijs = (List<OnderwijsSoort>) onderwijsSoortRepository.findByOndCode(ondCode);
		List<Gediplomeerden> gediplomeerden = null;
		for(OnderwijsSoort soort : onderwijs){
			gediplomeerden = (List<Gediplomeerden>) gediplomeerdenRepository.findByOndCode(soort);	
		}
				
		return gediplomeerden;
	}
	
}