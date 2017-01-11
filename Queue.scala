/** Une classe liste FIFO. */
case class Queue[T](in:List[T] = Nil, out:List[T] = Nil) {

	/** Ajoute un élément `x` en tête. */
	def enqueue(x:T):Queue[T] = {
		Queue(x :: in, out)
	}

	/** Retire le dernier élément. */
	def dequeue():(T,Queue[T]) = in match {
		case Nil if(out.isEmpty) => throw new Exception("La queue est vide.")
		case Nil => (out.head, Queue(Nil, out.tail))
		case _ if(out.isEmpty) => {val tmp = in.reverse; (tmp.head, Queue(Nil, tmp.tail))}
		case _ => (out.head, Queue(in, out.tail))		
	}

	/** Accès au premier élément. */
	def head():T = dequeue._1

	/** Vrai si la liste est vide. */
	def isEmpty:Boolean = in.isEmpty && out.isEmpty
	
	/** Retourne la taille de la queue. */
	def length:Int = in.length + out.length
	
	/** Retourne le dernier element de la queue sans la modifier. */
	def rear():T = in match {
		// in.head est null dans ce cas, il faut le remplacer par NULL
		case Nil if(out.isEmpty) => in.head
		case Nil => out.reverse.head
		case _ => in.head	
	}
}
