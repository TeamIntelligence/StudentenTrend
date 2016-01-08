package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.GediplomeerdenVacatures;
import studententrend.model.dao.GediplomeerdenVacaturesRepository;

/**
 * Called with the 
 * 
 */
@RestController
@RequestMapping("/studenten/vacatures")
public class GediplomeerdenVacaturesController {
	
	@Autowired
	GediplomeerdenVacaturesRepository gediplomeerdenVacaturesRepository;
	
	@RequestMapping(
			value = "", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<GediplomeerdenVacatures> gediplomeerdenVacaturesAll() {
		List<GediplomeerdenVacatures> gediplomeerdenVacatures = (List<GediplomeerdenVacatures>) gediplomeerdenVacaturesRepository.findAll();
		
		for(GediplomeerdenVacatures gediplVac : gediplomeerdenVacatures){
			System.out.println(gediplVac.toString() + '\n');
		}
		
		return gediplomeerdenVacatures;
	}
	
	@RequestMapping(
			value = "/{ondCode}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<GediplomeerdenVacatures> gediplomeerdenVacaturesAll(@PathVariable("ondCode") String ondCode) {
		List<GediplomeerdenVacatures> gediplomeerdenVacatures = (List<GediplomeerdenVacatures>) gediplomeerdenVacaturesRepository.findByOndCode(ondCode);
		
		for(GediplomeerdenVacatures gediplVac : gediplomeerdenVacatures){
			System.out.println(gediplVac.toString() + '\n');
		}
		
		return gediplomeerdenVacatures;
	}
}