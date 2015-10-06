package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.WoonSituatie;
import studententrend.model.dao.WoonSituatieRepository;

@RestController
public class WoonSituatieController {

	@Autowired
	WoonSituatieRepository woonSituatieRepository;

	@RequestMapping(
			value = "/woonsituaties", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<WoonSituatie> woonSituatiesAll() {
		List<WoonSituatie> woonSituaties = (List<WoonSituatie>) woonSituatieRepository.findAll();
		
		for(WoonSituatie woonSituatie : woonSituaties){
			System.out.println(woonSituatie.toString() + '\n');
		}
		
		return woonSituaties;
	}
}