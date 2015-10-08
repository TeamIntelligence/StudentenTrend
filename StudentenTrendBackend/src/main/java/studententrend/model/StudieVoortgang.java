package studententrend.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name="STUDIE_VOORTGANGEN")
public class StudieVoortgang {
		@Id
		@Column(name = "SV_ID")
		private long svId;

		@ManyToOne
		@JoinColumn(name="ISCED_CODE")
		private IscedCode iscedCode;

		@ManyToOne
		@JoinColumn(name="JAARTAL")
		private Jaartal jaartal;

		@JoinColumn(name="AANTAL")
		private int aantal;

		@Column(name = "HBO_GEDIPLOMEERD_3_JAAR")
		private int hboGediplomeerd3Jaar;
		
		@Column(name = "HBO_GEDIPLOMEERD_4_JAAR")
		private int hboGediplomeerd4Jaar;
		
		@Column(name = "HBO_GEDIPLOMEERD_5_JAAR")
		private int hboGediplomeerd5Jaar;
		
		@Column(name = "HBO_GEDIPLOMEERD_6_JAAR")
		private int hboGediplomeerd6Jaar;
		
		@Column(name = "HBO_GEDIPLOMEERD_7_JAAR")
		private int hboGediplomeerd7Jaar;
		
		@Column(name = "HBO_GEDIPLOMEERD_8_JAAR")
		private int hboGediplomeerd8Jaar;
		
		@Column(name = "HBO_GEDIPLOMEERD_9_JAAR")
		private int hboGediplomeerd9Jaar;
		
		@Column(name = "WO_GEDIPLOMEERD_3_JAAR")
		private int woGediplomeerd3Jaar;

		@Column(name = "WO_GEDIPLOMEERD_4_JAAR")
		private int woGediplomeerd4Jaar;

		@Column(name = "WO_GEDIPLOMEERD_5_JAAR")
		private int woGediplomeerd5Jaar;

		@Column(name = "WO_GEDIPLOMEERD_6_JAAR")
		private int woGediplomeerd6Jaar;

		@Column(name = "WO_GEDIPLOMEERD_7_JAAR")
		private int woGediplomeerd7Jaar;

		@Column(name = "WO_GEDIPLOMEERD_8_JAAR")
		private int woGediplomeerd8Jaar;

		@Column(name = "WO_GEDIPLOMEERD_9_JAAR")
		private int woGediplomeerd9Jaar;

		@Column(name = "UITGESCHREVEN_1_JAAR")
		private int uitgeschreven1Jaar;

		@Column(name = "UITGESCHREVEN_2_JAAR")
		private int uitgeschreven2Jaar;

		@Column(name = "UITGESCHREVEN_3_JAAR")
		private int uitgeschreven3Jaar;

		@Column(name = "UITGESCHREVEN_4_JAAR")
		private int uitgeschreven4Jaar;

		@Column(name = "UITGESCHREVEN_5_JAAR")
		private int uitgeschreven5Jaar;

		@Column(name = "UITGESCHREVEN_6_JAAR")
		private int uitgeschreven6Jaar;

		@Column(name = "UITGESCHREVEN_7_JAAR")
		private int uitgeschreven7Jaar;

		@Column(name = "UITGESCHREVEN_8_JAAR")
		private int uitgeschreven8Jaar;

		@Column(name = "INGESCHREVEN_2_JAAR")
		private int ingeschreven2Jaar;

		@Column(name = "INGESCHREVEN_3_JAAR")
		private int ingeschreven3Jaar;

		@Column(name = "INGESCHREVEN_4_JAAR")
		private int ingeschreven4Jaar;

		@Column(name = "INGESCHREVEN_5_JAAR")
		private int ingeschreven5Jaar;

		@Column(name = "INGESCHREVEN_6_JAAR")
		private int ingeschreven7Jaar;

		@Column(name = "INGESCHREVEN_8_JAAR")
		private int ingeschreven8Jaar;

		@Column(name = "INGESCHREVEN_9_JAAR")
		private int ingeschreven9Jaar;
		
		protected StudieVoortgang(){}
		
		public StudieVoortgang(long svId, IscedCode iscedCode, Jaartal jaartal, int aantal, int hboGediplomeerd3Jaar,
				int hboGediplomeerd4Jaar, int hboGediplomeerd5Jaar, int hboGediplomeerd6Jaar, int hboGediplomeerd7Jaar,
				int hboGediplomeerd8Jaar, int hboGediplomeerd9Jaar, int woGediplomeerd3Jaar, int woGediplomeerd4Jaar,
				int woGediplomeerd5Jaar, int woGediplomeerd6Jaar, int woGediplomeerd7Jaar, int woGediplomeerd8Jaar,
				int woGediplomeerd9Jaar, int uitgeschreven1Jaar, int uitgeschreven2Jaar, int uitgeschreven3Jaar,
				int uitgeschreven4Jaar, int uitgeschreven5Jaar, int uitgeschreven6Jaar, int uitgeschreven7Jaar,
				int uitgeschreven8Jaar, int ingeschreven2Jaar, int ingeschreven3Jaar, int ingeschreven4Jaar,
				int ingeschreven5Jaar, int ingeschreven7Jaar, int ingeschreven8Jaar, int ingeschreven9Jaar) {
			this.svId = svId;
			this.iscedCode = iscedCode;
			this.jaartal = jaartal;
			this.aantal = aantal;
			this.hboGediplomeerd3Jaar = hboGediplomeerd3Jaar;
			this.hboGediplomeerd4Jaar = hboGediplomeerd4Jaar;
			this.hboGediplomeerd5Jaar = hboGediplomeerd5Jaar;
			this.hboGediplomeerd6Jaar = hboGediplomeerd6Jaar;
			this.hboGediplomeerd7Jaar = hboGediplomeerd7Jaar;
			this.hboGediplomeerd8Jaar = hboGediplomeerd8Jaar;
			this.hboGediplomeerd9Jaar = hboGediplomeerd9Jaar;
			this.woGediplomeerd3Jaar = woGediplomeerd3Jaar;
			this.woGediplomeerd4Jaar = woGediplomeerd4Jaar;
			this.woGediplomeerd5Jaar = woGediplomeerd5Jaar;
			this.woGediplomeerd6Jaar = woGediplomeerd6Jaar;
			this.woGediplomeerd7Jaar = woGediplomeerd7Jaar;
			this.woGediplomeerd8Jaar = woGediplomeerd8Jaar;
			this.woGediplomeerd9Jaar = woGediplomeerd9Jaar;
			this.uitgeschreven1Jaar = uitgeschreven1Jaar;
			this.uitgeschreven2Jaar = uitgeschreven2Jaar;
			this.uitgeschreven3Jaar = uitgeschreven3Jaar;
			this.uitgeschreven4Jaar = uitgeschreven4Jaar;
			this.uitgeschreven5Jaar = uitgeschreven5Jaar;
			this.uitgeschreven6Jaar = uitgeschreven6Jaar;
			this.uitgeschreven7Jaar = uitgeschreven7Jaar;
			this.uitgeschreven8Jaar = uitgeschreven8Jaar;
			this.ingeschreven2Jaar = ingeschreven2Jaar;
			this.ingeschreven3Jaar = ingeschreven3Jaar;
			this.ingeschreven4Jaar = ingeschreven4Jaar;
			this.ingeschreven5Jaar = ingeschreven5Jaar;
			this.ingeschreven7Jaar = ingeschreven7Jaar;
			this.ingeschreven8Jaar = ingeschreven8Jaar;
			this.ingeschreven9Jaar = ingeschreven9Jaar;
		}

		public long getSvId() {
			return svId;
		}

		public IscedCode getIscedCode() {
			return iscedCode;
		}

		public int getJaartal() {
			return jaartal.getJaartal();
		}

		public int getAantal() {
			return aantal;
		}

		public int getHboGediplomeerd3Jaar() {
			return hboGediplomeerd3Jaar;
		}

		public int getHboGediplomeerd4Jaar() {
			return hboGediplomeerd4Jaar;
		}

		public int getHboGediplomeerd5Jaar() {
			return hboGediplomeerd5Jaar;
		}

		public int getHboGediplomeerd6Jaar() {
			return hboGediplomeerd6Jaar;
		}

		public int getHboGediplomeerd7Jaar() {
			return hboGediplomeerd7Jaar;
		}

		public int getHboGediplomeerd8Jaar() {
			return hboGediplomeerd8Jaar;
		}

		public int getHboGediplomeerd9Jaar() {
			return hboGediplomeerd9Jaar;
		}

		public int getWoGediplomeerd3Jaar() {
			return woGediplomeerd3Jaar;
		}

		public int getWoGediplomeerd4Jaar() {
			return woGediplomeerd4Jaar;
		}

		public int getWoGediplomeerd5Jaar() {
			return woGediplomeerd5Jaar;
		}

		public int getWoGediplomeerd6Jaar() {
			return woGediplomeerd6Jaar;
		}

		public int getWoGediplomeerd7Jaar() {
			return woGediplomeerd7Jaar;
		}

		public int getWoGediplomeerd8Jaar() {
			return woGediplomeerd8Jaar;
		}

		public int getWoGediplomeerd9Jaar() {
			return woGediplomeerd9Jaar;
		}

		public int getUitgeschreven1Jaar() {
			return uitgeschreven1Jaar;
		}

		public int getUitgeschreven2Jaar() {
			return uitgeschreven2Jaar;
		}

		public int getUitgeschreven3Jaar() {
			return uitgeschreven3Jaar;
		}

		public int getUitgeschreven4Jaar() {
			return uitgeschreven4Jaar;
		}

		public int getUitgeschreven5Jaar() {
			return uitgeschreven5Jaar;
		}

		public int getUitgeschreven6Jaar() {
			return uitgeschreven6Jaar;
		}

		public int getUitgeschreven7Jaar() {
			return uitgeschreven7Jaar;
		}

		public int getUitgeschreven8Jaar() {
			return uitgeschreven8Jaar;
		}

		public int getIngeschreven2Jaar() {
			return ingeschreven2Jaar;
		}

		public int getIngeschreven3Jaar() {
			return ingeschreven3Jaar;
		}

		public int getIngeschreven4Jaar() {
			return ingeschreven4Jaar;
		}

		public int getIngeschreven5Jaar() {
			return ingeschreven5Jaar;
		}

		public int getIngeschreven7Jaar() {
			return ingeschreven7Jaar;
		}

		public int getIngeschreven8Jaar() {
			return ingeschreven8Jaar;
		}

		public int getIngeschreven9Jaar() {
			return ingeschreven9Jaar;
		}		
}
