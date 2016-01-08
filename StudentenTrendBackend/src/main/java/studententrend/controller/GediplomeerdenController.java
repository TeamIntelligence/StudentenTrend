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

/**
 * Used to return all the data we have for the graduated students.
 * All the functions within this class are called when we use the base url including /studenten/(RequestMapping$value).
 * 
 */
@RestController
@RequestMapping("/studenten")
public class GediplomeerdenController {
	@Autowired
	GediplomeerdenRepository gediplomeerdenRepository;

	@Autowired
	OnderwijsSoortRepository onderwijsSoortRepository;
	

	/**
	 * Gets all the graduated students from the database
	 * @return All the graduated students
	 */	
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

	/**
	 * Gets all the graduated students based on the ondCode send as parameter inside the url
	 * @param ondCode (String) - The ondCode used to filter all the graduated students
	 * @return The filtered graduated students
	 */
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
			gediplomeerden = (List<Gediplomeerden>) gediplomeerdenRepository.findByOndCode(soort.getOndCode());	
		}
				
		return gediplomeerden;
	}	
}