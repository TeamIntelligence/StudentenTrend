package studententrend.controller;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.Plaats;
import studententrend.model.Provincie;
import studententrend.model.dao.PlaatsRepository;
import studententrend.model.dao.ProvincieRepository;

@RestController
public class ProvincieController {
	@Autowired
	ProvincieRepository provincieRepository;

	@RequestMapping(
			value = "/provincies", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Provincie> plaatsenAll() {
		List<Provincie> provincies = (List<Provincie>) provincieRepository.findAll();
		
		
		for(Provincie provincie : provincies){
			System.out.println(provincie.toString() + '\n');
		}
		
		return provincies;
	}
	
	@RequestMapping(
			value = "/provincies/{provId:\\d+}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Provincie> plaatsenByPlaatsId(@PathVariable("provId") int provId) {
		List<Provincie> provincies = (List<Provincie>) provincieRepository.findByProvId(provId);
		

		for(Provincie provincie : provincies){
			System.out.println(provincie.toString() + '\n');
		}

		return provincies;
	}

	
	@RequestMapping(
			value = "/provincies/{provNaam:(?!^\\d+$)^.+$}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Provincie> plaatsenByPlaatsNaam(@PathVariable("provNaam") String provNaam) {
		provNaam = provNaam.replaceAll("_", " ");
		
		List<Provincie> provincies = (List<Provincie>) provincieRepository.findByProvNaam(provNaam);
		
		for(Provincie provincie : provincies){
			System.out.println(provincie.toString() + '\n');
		}
		
		return provincies;
	}
}