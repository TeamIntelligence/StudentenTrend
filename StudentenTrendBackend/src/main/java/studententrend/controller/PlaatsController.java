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
public class PlaatsController {
	@Autowired
	PlaatsRepository plaatsRepository;

	@Autowired
	ProvincieRepository provincieRepository;

	@RequestMapping(
			value = "/plaatsen", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Plaats> plaatsenAll() {
		List<Plaats> plaatsen = (List<Plaats>) plaatsRepository.findAll();
		
		
		for(Plaats plaats : plaatsen){
			System.out.println(plaats.toString() + '\n');
		}
		
		return plaatsen;
	}
	
	@RequestMapping(
			value = "/plaatsen/{plaatsId:\\d+}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Plaats> plaatsenByPlaatsId(@PathVariable("plaatsId") int plaatsId) {
		List<Plaats> plaatsen = (List<Plaats>) plaatsRepository.findByPlaatsId(plaatsId);
		

		for(Plaats plaats : plaatsen){
			System.out.println(plaats.toString() + '\n');
		}

		return plaatsen;
	}

	
	@RequestMapping(
			value = "/plaatsen/{plaatsNaam:(?!^\\d+$)^.+$}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Plaats> plaatsenByPlaatsNaam(@PathVariable("plaatsNaam") String plaatsNaam) {
		plaatsNaam = plaatsNaam.replaceAll("_", " ");
		
		List<Plaats> plaatsen = (List<Plaats>) plaatsRepository.findByPlaatsNaam(plaatsNaam);
		
		for(Plaats plaats : plaatsen){
			System.out.println(plaats.toString() + '\n');
		}
		
		return plaatsen;
	}
	
	@RequestMapping(
			value = "/plaatsen/prov/{provId:\\d+}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Plaats> plaatsenByProvId(@PathVariable("provId") int provId) {
		System.out.println('\n' + provId + '\n');
		List<Provincie> provincies = (List<Provincie>) provincieRepository.findByProvId(provId);
		ArrayList<Plaats> plaatsen = new ArrayList<Plaats>();
		
		for(Provincie prov : provincies) {
			plaatsen.addAll(plaatsRepository.findByProvincie(prov.getProvNaam()));
		}
		
		
		for(Plaats plaats : plaatsen){
			System.out.println(plaats.toString() + '\n');
		}
		
		return plaatsen;
	}
}