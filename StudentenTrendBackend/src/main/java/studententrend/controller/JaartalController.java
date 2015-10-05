package studententrend.controller;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.Jaartal;
import studententrend.model.dao.JaartalRepository;

@RestController
public class JaartalController {
	@Autowired
	JaartalRepository jaartalRepository;

	@RequestMapping(
			value = "/jaartallen", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Jaartal> jaartalAll() {
		List<Jaartal> allYears = (List<Jaartal>) jaartalRepository.findAll();
		
		for(Jaartal year : allYears){
			System.out.println(year.toString() + '\n');
		}
		
		return allYears;
	}
	

	@RequestMapping(
			value = "/jaartallen/{jaartal}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Jaartal> jaartalByJaartal(@PathVariable("jaartal") int jaartal) {
		List<Jaartal> allYears = (List<Jaartal>) jaartalRepository.findByJaartal(jaartal);
		
		for(Jaartal year : allYears){
			System.out.println(year.toString() + '\n');
		}
		
		return allYears;
	}
	

	@RequestMapping(
			value = "/jaartallen/{from}/{to}", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Jaartal> jaartalFromTo(@PathVariable("from") int from, @PathVariable("to") int to) {
		List<Jaartal> allYears = (List<Jaartal>) jaartalRepository.findAll();
		ArrayList<Jaartal> years = new ArrayList<Jaartal>();
		
		if(from > to) {
			return new ArrayList<Jaartal>();
		}
		
		for(Jaartal year : allYears) {
			if(year.getJaartal() >= from && year.getJaartal() <= to) {
				years.add(year);
			}
		}
		
		for(Jaartal year : years){
			System.out.println(year.toString() + '\n');
		}
		
		return years;
	}
}
