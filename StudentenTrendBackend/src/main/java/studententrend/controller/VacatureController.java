package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.Vacature;
import studententrend.model.dao.VacatureRepository;

@RestController
public class VacatureController {

	@Autowired
	VacatureRepository vacatureRepository;

	@RequestMapping(
			value = "/vacatures", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<Vacature> vacaturesAll() {
		List<Vacature> vacatures = (List<Vacature>) vacatureRepository.findAll();
		
		for(Vacature vacature : vacatures){
			System.out.println(vacature.toString() + '\n');
		}
		
		return vacatures;
	}
}