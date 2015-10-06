package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.HoIngeschrevene;
import studententrend.model.dao.HoIngeschreveneRepository;

@RestController
public class HoIngeschreveneController {
	@Autowired
	HoIngeschreveneRepository hoIngeschrevenenRepository;

	@RequestMapping(
			value = "/ingeschrevenen/ho", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<HoIngeschrevene> hoGediplomeerdenAll() {
		List<HoIngeschrevene> hoIngeschrevenen = (List<HoIngeschrevene>) hoIngeschrevenenRepository.findAll();
		
		for(HoIngeschrevene hoIngeschreven : hoIngeschrevenen){
			System.out.println(hoIngeschreven.toString() + '\n');
		}
		
		return hoIngeschrevenen;
	}
	
}