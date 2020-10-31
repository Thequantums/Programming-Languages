import java.util.concurrent.atomic.AtomicLongArray;

class AcmeSafeState implements State {
    private AtomicLongArray value;

    AcmeSafeState(int length) { value = new AtomicLongArray(length); }

    public int size() { return value.length(); }

    public long[] current() { 
      long[] temp = new long[value.length()];
      for (int i =0; i < temp.length; i++) {
	    temp[i] = (long) value.get(i);
	}
        return temp;
      }

    public void swap(int i, int j) {
       value.getAndIncrement(i);
       value.getAndDecrement(j);
    }
}
