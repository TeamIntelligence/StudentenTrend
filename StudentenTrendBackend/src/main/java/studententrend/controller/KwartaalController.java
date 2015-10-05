package studententrend.controller;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.Kwartaal;
import studententrend.model.dao.KwartaalRepository;

@RestController
public class KwartaalController {
	@Autowired
	KwartaalRepository kwartaalRepository;

	@RequestMapping(
			value = "/kwartalen", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Kwartaal> kwartaalAll() {
		List<Kwartaal> kwartalen = (List<Kwartaal>) kwartaalRepository.findAll();
		
		for(Kwartaal kwartaal : kwartalen){
			System.out.println(kwartaal.toString() + '\n');
		}
		
		return kwartalen;
	}
	

	@RequestMapping(
			value = "/kwartalen/{kwartaal}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Kwartaal> jaartalByJaartal(@PathVariable("kwartaal") int kwartaalnr) {
		List<Kwartaal> kwartalen = (List<Kwartaal>) kwartaalRepository.findByKwartaal(kwartaalnr);
		
		for(Kwartaal kwartaal : kwartalen){
			System.out.println(kwartaal.toString() + '\n');
		}
		
		return kwartalen;
	}
	
	@RequestMapping(
			value = "/kwartalen/{from}/{to}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Kwartaal> kwartaalFromTo(@PathVariable("from") int from, @PathVariable("to") int to) {
		List<Kwartaal> kwartalen = (List<Kwartaal>) kwartaalRepository.findAll();
		ArrayList<Kwartaal> selKwartalen = new ArrayList<Kwartaal>();
		
		if(from > to) {
			return new ArrayList<Kwartaal>();
		}
		
		for(Kwartaal kwartaal : kwartalen) {
			if(kwartaal.getKwartaal() >= from && kwartaal.getKwartaal() <= to) {
				selKwartalen.add(kwartaal);
			}
		}
		
		for(Kwartaal kwartaal : selKwartalen){
			System.out.println(kwartaal.toString() + '\n');
		}
		
		return selKwartalen;
	}
}
